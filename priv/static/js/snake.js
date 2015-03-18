$(function() {
    // Canvas
    var c = $("#canvas").get(0);
    var canvas = c.getContext("2d");
    canvas.fillRect(0, 0, c.width, c.height);

    // Code to handle keyboard input
    var KEY_UP = 38, KEY_DOWN = 40, KEY_LEFT = 37, KEY_RIGHT = 39;
    
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
            default:
                break;
        }
    }
    
    var notify_input = function(channel, dir) {
        channel.send("new_input", {direction: dir});
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
    canvas.fillStyle = "black";
    canvas.fillRect(position.x * size, position.y * size, size, size);
}

function draw_snake(canvas, snake) {
    canvas.fillStyle = snake.color;
    canvas.fillRect(snake.x * size, snake.y * size, size, size);
}

function draw_insect(canvas, insect) {
    console.log(insect)
    console.log("x =" + insect.x);
    console.log("y =" + insect.y);
    canvas.fillStyle = "red";
    canvas.fillRect(insect.x * size, insect.y * size, size, size);
}

function update_score(score) { 
    var currentScore = $("#score");
    var newScore = parseInt(currentScore.html()) + parseInt(score.score);
    currentScore.html(newScore);
}
