package code.comet

import code.model.Position

class GoogleMapsClient(key: String) {

  def getInitialScript = 
    sourcedJavascript("http://maps.googleapis.com/maps/api/js?key=" + key + "&sensor=true")

  def getStartScript(zoom: Int = 14, id: String = "map_canvas") = 
    javascript("""
      var google_map = 'undefined'

      function zoomTo(lat, lon) {
        google_map.panTo(new google.maps.LatLng(lat, lon) );
      }

      function initializeMaps(lat, lon) {
        google_map = new google.maps.Map(document.getElementById("%s"), {
          center: new google.maps.LatLng(lat, lon), 
          zoom: %d,
          mapTypeId: google.maps.MapTypeId.ROADMAP
        });
      }""".format(id, zoom))
  
  def getCanvasScript(position: Position, id: String) =
    <div id={id} style="width:100%; height:100%">
      { javascript("initializeMaps(" + position.latitude + ", " + position.longitude + ")") }
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
