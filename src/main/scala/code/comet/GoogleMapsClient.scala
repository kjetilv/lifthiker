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

  def zoomToCmd(position: Position, id: String, zoom: Int = 16) = 
    Call("zoomTo", JsRaw(position.latitude.toString), JsRaw(position.longitude.toString))
  
  def getCanvasScript(position: Position, zoom: Int = 16) =
    <div id="map_canvas" style="width:100%; height:100%">{ 
      javascript("zoomTo(" + position.latitude + ", " + position.longitude + ", " + zoom + ")") 
      }</div>
  
  def getURL(position: Position) =
    "https://maps.google.com/?ie=UTF8&amp;hq=&amp;hnear=Oslo,+Norway&amp;ll=" +
      position.latitude + "," + position.longitude + 
      "&amp;spn=0.54042,1.388397&amp;t=h&amp;z=10&amp;output=embed"
  
  private def javascript(script: String) = <script type="text/javascript">{script}</script>
  
  private def sourcedJavascript(src: String) = <script type="text/javascript" src={src}></script>
}
