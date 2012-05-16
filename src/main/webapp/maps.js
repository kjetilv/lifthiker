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
    } 
}
