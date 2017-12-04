function parseVideoId(url) {
  var p = /^(?:https?:\/\/)?(?:m\.|www\.)?(?:youtu\.be\/|youtube\.com\/(?:embed\/|v\/|watch\?v=|watch\?.+&v=))((\w|-){11})(?:\S+)?$/;
  if(url.match(p)){
    return url.match(p)[1];
  }
  return false;
}

function parseVideoTime(url) {
  var p = /[?&]t=(\d+m)?(\d+)s/;
  if(url.match(p)){
    m = url.match(p)[1] ? parseInt(url.match(p)[1], 10) : 0;
    s = parseInt(url.match(p)[2], 10);
    if(isNaN(m) || isNaN(s)){
      return null;
    }
    return m*60 + s;
  }
  return null;
}


function requestVideo(){
  vidtxt = document.getElementById('vidtxt');
  url = vidtxt.value;
  vidId = parseVideoId(url);
  if(!vidId){
    document.getElementById('errors').innerHTML='<p>Error: invalid youtube url</p>';
    return;
  }
  vidtxt.blur();
  document.getElementById("vid").innerHTML='<div class="spinner"></div>';
  vidTime = parseVideoTime(url);
  vid = { videoId: vidId, videoTime: vidTime };
  postWatch(vid, function(newVid){
    vidtxt.value="";
    document.getElementById('vid').innerHTML='<iframe width="560" height="315" src="https://www.youtube.com/embed/' + newVid.videoId + '?autoplay=1' + (newVid.videoTime ? "&start=" + newVid.videoTime : "") + '" frameborder="0" allowfullscreen></iframe>';
  }, function(err){
    document.getElementById('errors').innerHTML='<p>Error: can\'t connect to server</p>';
    document.getElementById("vid").innerHTML='<div id="errorsign">⚠</div>';
  });
}

