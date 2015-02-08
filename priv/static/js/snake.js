$(function() {
    var c = $("#canvas").get(0);
    var canvas = c.getContext("2d");
    canvas.fillRect(0, 0, c.width, c.height);

    // Create websocket
    var socket = new Phoenix.Socket("/ws");

    // Websocket logic:
    socket.join("snake", {}, function(channel) {
        channel.on("draw_snake", function(msg) {
            draw_snake(canvas, msg);
        });

        channel.on("draw_insect", function(msg) {
            draw_insect(canvas, msg);
        });

        channel.on("score", function(msg) {
            update_score(msg);
        });
    });
});


// TODO use assign to store board # on the connection.

var size = 20;

// TODO maybe remove function and work with diffs to clear screen.
function clear_screen(canvas) {
    var c = $("#canvas").get(0); // TODO refactor this later.
    canvas.fillStyle = "black";
    canvas.fillRect(0, 0, c.width, c.height);
}

function draw_snake(canvas, snake) {
    canvas.fillStyle = snake.color;
    canvas.fillRect(snake.x, snake.y, size, size);
    canvas.fillStyle = "white";
    canvas.rect(snake.x, snake.y, size, size);
}

function draw_insect(canvas, insect) {
    canvas.fillStyle = "red";
    canvas.fillRect(insect.x, insect.y, size, size);
}

function update_score(score) { // score currently not used..
    var currentScore = $("#score");
    var newScore = parseInt(currentScore.html()) + 1;
    currentScore.html(newScore);
}

// etc..
