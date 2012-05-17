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

function addMarkerOn(lat, lon, name) {
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
                raiseOnDrag: false,
                title: name,
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
        var position = markers[i].getPosition();
        if (aboutSame(lat, position.lat()) && 
            aboutSame(lon, position.lon())) {
            return i;
        }
    }
    return -1;
}

function aboutSame(one, two) {
    return Math.abs(one / two - 1) < 0.000001;
}