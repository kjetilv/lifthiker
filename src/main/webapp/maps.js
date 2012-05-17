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
