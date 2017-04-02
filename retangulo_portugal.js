var colors = [
  3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
  3,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,3,
  3,6,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,6,3,
  3,6,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,6,3,
  3,6,3,0,3,3,3,0,3,3,3,0,3,3,3,0,3,3,3,0,3,0,3,0,3,3,3,0,3,3,3,0,3,0,0,0,0,0,3,3,3,0,3,3,3,0,3,3,3,0,3,3,3,0,3,0,0,0,3,0,3,0,3,3,3,0,3,0,3,0,3,0,3,6,3,
  3,6,3,0,3,0,3,0,3,0,3,0,3,0,3,0,0,3,0,0,3,0,3,0,3,0,0,0,3,0,3,0,3,0,0,0,0,0,3,0,0,0,3,0,3,0,3,0,3,0,3,0,3,0,3,0,0,0,3,0,3,0,3,0,3,0,3,0,3,0,3,0,3,6,3,
  3,6,3,0,3,3,3,0,3,0,3,0,3,3,0,0,0,3,0,0,3,0,3,0,3,0,3,0,3,3,3,0,3,0,0,0,0,0,3,0,0,0,3,3,3,0,3,3,0,0,3,3,3,0,3,0,0,0,3,3,3,0,3,0,3,0,3,0,3,0,3,0,3,6,3,
  3,6,3,0,3,0,0,0,3,0,3,0,3,0,3,0,0,3,0,0,3,0,3,0,3,0,3,0,3,0,3,0,3,0,0,0,0,0,3,0,0,0,3,0,3,0,3,0,3,0,3,0,3,0,3,0,0,0,3,0,3,0,3,0,3,0,0,0,0,0,0,0,3,6,3,
  3,6,3,0,3,0,0,0,3,3,3,0,3,0,3,0,0,3,0,0,3,3,3,0,3,3,3,0,3,0,3,0,3,3,3,0,0,0,3,3,3,0,3,0,3,0,3,0,3,0,3,0,3,0,3,3,3,0,3,0,3,0,3,3,3,0,3,0,3,0,3,0,3,6,3,  
  3,6,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,6,3,
  3,6,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,6,3,
  3,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,3,7,3,10,10,10,10,10,10,10,10,10,10,10,10,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,3,6,6,6,6,6,6,6,3,
  3,6,6,6,6,6,6,6,6,6,3,3,3,6,6,3,3,3,6,6,6,3,3,3,6,6,3,3,3,6,6,6,6,3,7,3,10,10,10,10,10,10,10,10,10,10,10,10,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,3,6,6,6,6,6,6,6,3,
  3,6,6,6,6,6,6,6,6,6,3,6,6,6,6,3,6,6,3,6,6,3,6,6,6,6,3,6,3,6,6,6,6,3,7,3,10,10,10,10,10,10,10,10,10,10,10,10,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,3,6,6,6,6,6,6,6,3,
  3,6,6,6,6,6,6,6,6,6,3,3,6,6,6,3,6,6,3,6,6,3,3,6,6,6,3,3,6,6,6,6,6,3,7,3,10,10,10,10,10,10,10,10,10,10,10,10,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,3,6,6,6,6,6,6,6,3,
  3,6,6,6,6,6,6,6,6,6,3,6,6,6,6,3,6,6,3,6,6,3,6,6,6,6,3,6,3,6,6,6,6,3,7,3,10,10,10,10,10,10,10,10,10,10,10,10,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,3,6,6,6,6,6,6,6,3,
  3,6,6,6,6,6,6,6,6,6,3,3,3,6,6,3,3,3,6,6,6,3,3,3,6,6,3,6,3,6,6,6,6,3,7,3,10,10,10,10,10,10,10,10,10,8,8,8,8,8,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,3,6,6,6,6,6,6,6,3,
  3,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,3,7,3,10,10,10,10,10,10,10,10,8,8,8,8,8,8,8,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,3,6,6,6,6,6,6,6,3,
  3,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,3,7,3,10,10,10,10,10,10,10,8,8,5,5,5,5,5,8,8,5,5,5,5,5,5,5,5,5,5,5,5,5,5,3,6,6,6,6,6,6,6,3,
  3,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,3,7,3,10,10,10,10,10,10,10,8,8,5,0,13,0,5,8,8,5,5,5,5,5,5,5,5,5,5,5,5,5,5,3,6,6,6,6,6,6,6,3,
  3,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,3,7,3,10,10,10,10,10,10,10,8,8,5,13,13,13,5,8,8,5,5,5,5,5,5,5,5,5,5,5,5,5,5,3,6,6,6,6,6,6,6,3,
  3,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,3,3,3,3,3,3,6,6,6,6,6,6,6,6,6,3,7,3,10,10,10,10,10,10,10,8,8,5,0,13,0,5,8,8,5,5,5,5,5,5,5,5,5,5,5,5,5,5,3,6,6,6,6,6,6,6,3,
  3,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,3,3,3,12,12,12,12,3,3,3,6,6,6,6,6,6,6,3,7,3,10,10,10,10,10,10,10,8,8,5,5,0,5,5,8,8,5,5,5,5,5,5,5,5,5,5,5,5,5,5,3,6,6,6,6,6,6,6,3,
  3,6,6,6,6,6,6,6,6,6,6,6,6,6,6,3,3,12,12,12,12,12,12,12,12,3,3,6,6,6,6,6,6,3,7,3,10,10,10,10,10,10,10,10,8,8,5,5,5,8,8,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,3,6,6,6,6,6,6,6,3,
  3,6,6,6,6,6,6,6,6,6,6,6,6,6,3,3,12,12,12,12,12,12,12,12,12,12,3,3,6,6,6,6,6,3,7,3,10,10,10,10,10,10,10,10,10,8,8,8,8,8,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,3,6,6,6,6,6,6,6,3,
  3,6,6,6,6,6,6,6,6,6,6,6,6,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,6,6,6,3,7,3,10,10,10,10,10,10,10,10,10,10,10,10,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,3,6,6,6,6,6,6,6,3,
  3,6,6,6,6,6,6,6,6,6,6,6,6,3,3,0,0,0,3,3,3,3,3,0,0,0,3,3,3,6,6,6,6,3,7,3,10,10,10,10,10,10,10,10,10,10,10,10,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,3,6,6,6,6,6,6,6,3,
  3,6,6,6,6,6,6,6,6,6,6,6,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,6,6,6,3,7,3,10,10,10,10,10,10,10,10,10,10,10,10,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,3,6,6,6,6,6,6,6,3,
  3,6,6,6,6,6,6,6,6,6,6,6,3,3,3,3,3,3,12,12,3,3,3,3,3,12,12,3,3,3,6,6,6,3,7,3,10,10,10,10,10,10,10,10,10,10,10,10,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,3,6,6,6,6,6,6,6,3,
  3,6,6,6,6,6,6,6,6,6,6,6,3,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,3,6,6,6,3,7,3,10,10,10,10,10,10,10,10,10,10,10,10,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,3,6,6,6,6,6,6,6,3,
  3,6,6,6,6,6,6,6,6,6,6,6,3,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,3,6,6,6,3,7,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,6,6,6,6,6,6,6,3,
  3,6,6,6,6,6,6,6,6,6,6,6,3,12,3,3,3,3,3,3,3,3,3,3,3,12,12,12,12,3,6,6,6,3,7,3,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,3,
  3,6,6,6,6,6,6,6,6,6,6,6,3,3,5,5,5,5,5,0,5,0,5,5,3,12,12,12,12,3,6,6,6,3,7,3,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,3,
];
var colorsABGR = [];

var maga = {
  x: 675,
  y: 391,
  width: 75,
  height: 33
};

var placed = 0;

// hooks
var client;
var canvasse;
var jQuery;

var test = 0;

r.placeModule("Maga", function(e){
  client = e("client");
  canvasse = e("canvasse");
  jQuery = e("jQuery");

  for(var i=0; i<client.palette.length; i++){
    colorsABGR[i] = client.getPaletteColorABGR(i);
  }

  // Start
  if(!test){
    attempt();
  } else {
    drawTestMaga();
  }
});

function attempt(){
  var toWait = client.getCooldownTimeRemaining();
  if(toWait === 0){
     for(var i=0; i<colors.length; i++){
        if(colors[i] === -1){
          continue;
        }
        var targetPoint = getPoint(i);
        var pixelColor = getPixel(targetPoint.x, targetPoint.y);
        if(pixelColor !== colorsABGR[colors[i]]){
            client.setColor(colors[i]);
            client.drawTile(targetPoint.x, targetPoint.y);
            console.log('Pixel Placed at: (' + targetPoint.x + ',' + targetPoint.y + ')');
            break;
        }
     }
  }
  setTimeout(attempt, toWait + Math.round(Math.random() * 1500));
}

function drawTestMaga(){
  for(var i=0; i<colors.length; i++){
    if(colors[i] === -1){
      continue;
    }
    var targetPoint = getPoint(i);
    canvasse.drawTileAt(targetPoint.x, targetPoint.y, colorsABGR[colors[i]]);
  }
}

function getPoint(i){
  var x = i % maga.width;
  return {
    x: maga.x + x,
    y: maga.y + (i - x) / maga.width - maga.height
  };
}

function getPixel(x, y){
  return canvasse.writeBuffer[canvasse.getIndexFromCoords(x, y)];
}