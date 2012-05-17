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
