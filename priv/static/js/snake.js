$(function() {
    var c = $("#canvas").get(0);
    var canvas = c.getContext("2d");
    canvas.fillRect(0, 0, c.width, c.height);

    // Create websocket
    var socket = new Phoenix.Socket("/ws");

    // Websocket logic:
    socket.join("snake", {}, function(channel) {
        channel.on("draw_snake", function(msg) {
            drawSnake(canvas, msg);
        });

        channel.on("draw_insect", function(msg) {
            drawInsect(canvas, msg);
        });

        channel.on("score", function(msg) {
            updateScore(msg);
        });
    });
});

function drawSnake(canvas, snake) {
    // TODO
}

function drawInsect(canvas, insect) {
    // TODO
    // TODO use assign to store board # on the connection.
}

function updateScore(score) { // score currently not used..
    var currentScore = $("#score");
    var newScore = parseInt(currentScore.html()) + 1;
    currentScore.html(newScore);
}

// etc..
