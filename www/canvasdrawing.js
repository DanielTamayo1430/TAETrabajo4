var canvas = document.getElementById('canvas');
var context = canvas.getContext('2d');

var radius = 10;
var dragging = false;

var color = 'black';
//var font = 1;


context.lineWidth = radius * 2;

var putPoint = function(e){
    if(dragging){
        context.lineTo(e.offsetX, e.offsetY);
        context.stroke();
        context.beginPath();
        context.arc(e.offsetX, e.offsetY, radius, 0, Math.PI*2);
        context.fill();
        context.beginPath();
        context.moveTo(e.offsetX, e.offsetY);
        context.strokeStyle = color;
        context.fillStyle = color;
    }
}

var engage = function(e){
    dragging = true;
    putPoint(e);
}

var disengage = function(){
    dragging = false;
}

var changeColor = function(e){
    color = this.getAttribute('data-color');
}

/*var changeFont = function(e){
    font = this.getAttribute('data-size');
}*/


canvas.addEventListener('mousedown', engage);
canvas.addEventListener('mousemove', putPoint);
canvas.addEventListener('mouseup', disengage);

/////////////////////

var colors = document.getElementsByClassName('color');

for(var i = 0; i < colors.length; i++){
    colors[i].addEventListener('click', changeColor, false);
}

//

/*var fontSize = document.getElementsByClassName('size');

for(var i = 0; i < fontSize.length; i++){
    fontSize[i].addEventListener('click'. changeFont, false);
}*/


document.getElementById('CanvasClear').addEventListener("click", function(){

    var confirmClear = confirm('Estas seguro de borrar lo digitado ?');
    if (confirmClear == true) {
        context.fillStyle = "white";
        context.clearRect(0, 0, canvas.width, canvas.height);
        context.fillRect(0, 0, canvas.width, canvas.height);
        restore = new Array();
        resloc = -1;
    } else {}
});

