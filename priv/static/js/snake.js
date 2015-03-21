$(function() {
    // Canvas
    var c = $("#canvas").get(0);
    var canvas = c.getContext("2d");
    canvas.fillRect(0, 0, c.width, c.height);

    // Code to handle keyboard input
    var KEY_UP = 40, KEY_DOWN = 38, 
        KEY_LEFT = 37, KEY_RIGHT = 39, KEY_SPACE = 32;
    
    var send_input = function(channel, key_code) {
        switch(key_code) {
            case KEY_UP:
                notify_input(channel, "up");
                break;
            case KEY_DOWN:
                notify_input(channel, "down");
                break;
            case KEY_RIGHT:
                notify_input(channel, "right");
                break;
            case KEY_LEFT:
                notify_input(channel, "left");
                break;
            case KEY_SPACE:
                notify_input(channel, "space");
            default:
                break;
        }
    }
    
    var notify_input = function(channel, key_input) {
        channel.send("new_input", {input: key_input});
    }

    // Websocket logic
    var socket = new Phoenix.Socket("/ws");

    socket.join("snake", {}, function(channel) {
        channel.on("draw_snake", function(msg) {
            draw_snake(canvas, msg);
        });

        channel.on("draw_insect", function(msg) {
            draw_insect(canvas, msg);
        });

        channel.on("clear_tile", function(msg) {
            clear_tile(canvas, msg);
        });

        channel.on("score", function(msg) {
            update_score(msg);
        });

        channel.on("reset_score", function(msg) {
            reset_score();
        });
        
        clear_screen(canvas);
        channel.send("draw_all", {});

        // Register player input
        $(window).keydown(function(key_event) {
            send_input(channel, key_event.keyCode);
        });
    });
});

var size = 40; // size of each coordinate.

// TODO maybe remove function and work with diffs to clear screen.
function clear_screen(canvas) {
    var c = $("#canvas").get(0);
    canvas.fillStyle = "black";
    canvas.fillRect(0, 0, c.width, c.height);
}

function clear_tile(canvas, position) {
    draw_rect(canvas, position.x, position.y, "black");
}

function draw_snake(canvas, snake) {
    draw_rect(canvas, snake.x, snake.y, snake.color);
}

function draw_insect(canvas, insect) {
    draw_rect(canvas, insect.x, insect.y, "red");
}

function update_score(score) { 
    var current_score = $("#score");
    var high_score = $("#highscore");
    var highest_score = parseInt(high_score.html());
    var new_score = parseInt(current_score.html()) + parseInt(score.score);
    
    current_score.html(new_score);
    if (new_score > highest_score) {
        high_score.html(new_score);
    }
}

function reset_score() {
    var score = $("#score");
    score.html(0);
}

function draw_rect(canvas, x, y, color) {
    canvas.fillStyle = color;
    canvas.strokeStyle = "black";
    canvas.lineWidth = "1";
    canvas.fillRect(x * size, y * size, size, size);
    canvas.strokeRect(x * size, y * size, size, size);
}
