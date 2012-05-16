package code.comet

import code.model.Position
import net.liftweb.http.js.JE.{JsRaw, Call}

class GoogleMapsClient(key: String) {

  def getInitialScript = 
    sourcedJavascript("http://maps.googleapis.com/maps/api/js?key=" + key + "&sensor=true")

  def getStartScript(id: String = "map_canvas") = 
    javascript("""
var google_map = 0;

function get_map() {
    if (google_map == 0) {
        throw MapNotInitialized;
    }
    return google_map;
}

function zoomTo(lat, lon) {
  get_map().panTo(new google.maps.LatLng(lat, lon));
}

function initializeMaps(lat, lon, z) {
    if (google_map == 0) {
        google_map = new google.maps.Map(document.getElementById("%s"), {
            center: new google.maps.LatLng(lat, lon), 
            zoom: z,
            mapTypeId: google.maps.MapTypeId.ROADMAP
        });
    } else {
        zoomTo(lat, lon);
    }
}
               """.format(id))
  
  def getCanvasCall(position: Position, id: String, zoom: Int = 16) = 
    Call("initializeMaps", JsRaw(position.latitude.toString), JsRaw(position.longitude.toString), JsRaw(zoom.toString))
  
  def getCanvasScript(position: Position, id: String, zoom: Int = 16) =
    <div id={id} style="width:100%; height:100%">
      { javascript("initializeMaps(" + position.latitude + ", " + position.longitude + ", " + zoom + ")") }
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
