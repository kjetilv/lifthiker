package code.comet

import code.model.Position
import net.liftweb.http.js.JE.{JsRaw, Call}

class GoogleMapsClient(key: String) {

  def getGoogleAPIScript = 
    sourcedJavascript("http://maps.googleapis.com/maps/api/js?key=" + key + "&sensor=false")

  def getStartScript = 
    javascript("""
var google_map = 0;

var markers = [];

function zoomTo(lat, lon, z) {
    if (google_map == 0) {
        var zoom_value = typeof(z) == 'undefined' ? 16 : z;
        google_map = new google.maps.Map(document.getElementById('map_canvas'), {
          center: new google.maps.LatLng(lat, lon), 
          zoom: zoom_value,
          mapTypeId: google.maps.MapTypeId.ROADMAP
        });
    } else {
        google_map.panTo(new google.maps.LatLng(lat, lon));
    }
}

function addMarkerOn(lat, lon) {
    if (google_map != 0) {
        var index = indexOf(lat, lon);
        if (index > 0) {
            for (var i = 0; i < markers.length; i++) {
                markers[i].setAnimation(i == index ? google.maps.Animation.BOUNCE : null);
            }
        } else {
            for (var j = 0; j < markers.length; j++) {
                markers[j].setAnimation(null);
            }
            markers.push(new google.maps.Marker({
                position: new google.maps.LatLng(lat, lon),
                animation: google.maps.Animation.BOUNCE,
                map: google_map
            }))
        }
    }
}

function removeMarker(lat, lon) {
    var index = indexOf(lat, lon);
    markers[index].setMap(null);
    markers.splice(index, index);
}

function indexOf(lat, lon) {
    for (var i = 0; i < markers.length; i++) {
        if (aboutSame(lat, markers[i].lat) && aboutSame(lon, markers[i].lon)) {
            return i;
        }
    }
    return -1;
}

function aboutSame(one, two) {
    return Math.abs(one / two) - 1 < 0.0001;
}               """)
  
  def getCanvasCall(position: Position, id: String, zoom: Int = 16) = 
    Call("zoomTo", JsRaw(position.latitude.toString), JsRaw(position.longitude.toString))
  
  def getCanvasScript(position: Position, zoom: Int = 16) =
    <div id="map_canvas" style="width:100%; height:100%">
      { 
        javascript("zoomTo(" + position.latitude + ", " + position.longitude + ", " + zoom + ")") 
      }
    </div>
  
  def getURL(position: Position) =
    "https://maps.google.com/?ie=UTF8&amp;hq=&amp;hnear=Oslo,+Norway&amp;ll=" +
      position.latitude + "," + position.longitude + 
      "&amp;spn=0.54042,1.388397&amp;t=h&amp;z=10&amp;output=embed"
  
  private def javascript(script: String) = 
    <script type="text/javascript">{script}</script>
  
  private def sourcedJavascript(src: String) = 
    <script type="text/javascript" src={src}></script>
}
