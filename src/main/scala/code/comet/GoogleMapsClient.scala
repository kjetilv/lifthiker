/*
 * Copyright 2012 Kjetil Valstadsve
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package code.comet

import code.model.Position
import net.liftweb.http.js.JE.{JsRaw, Call}

class GoogleMapsClient(key: String) {

  def getGoogleAPIScript = 
    sourcedJavascript("http://maps.googleapis.com/maps/api/js?key=" + key + "&sensor=false")

  def getStartScript = 
    javascript("""
/*
 * Copyright 2012 Kjetil Valstadsve
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

var google_map = 0;

var markers = [];

function selectStop(lat, lon, id, ajaxCall) {
    zoomTo(lat, lon); 
    addMarkerOn(lat, lon, id);
    ajaxCall();
}

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

function addMarkerOn(lat, lon, id) {
    if (google_map != 0) {
        var index = indexOf(id);
        if (index < 0) {
            for (var j = 0; j < markers.length; j++) {
                markers[j].m.setAnimation(null);
            }
            markers.push({
                id: id,
                m: new google.maps.Marker({
                    position: new google.maps.LatLng(lat, lon),
                    animation: google.maps.Animation.BOUNCE,
                    raiseOnDrag: false,
                    title: id.toString(),
                    map: google_map
                })
            })
        } else {
            for (var i = 0; i < markers.length; i++) {
                markers[i].m.setAnimation(i == index ? google.maps.Animation.BOUNCE : null);
            }
        }
    }
}

function removeMarker(id) {
    var index = indexOf(id);
    markers[index].m.setMap(null);
    markers.splice(index, index);
}

function indexOf(id) {
    for (var i = 0; i < markers.length; i++) {
        if (markers[i].id == id) {  
            return i;
        }
    }
    return -1;
}
               """)
  
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
