let video;
let duration;
let time_elapsed;
let seek;
let progress_bar;
let seekTooltip;
let videoControls;
let hide_control_threshold = 1000;
let vidControlsvidControlsTimeoutId;
let radioListTimeoutId;
let tl_button_wrapper;
let tl_choice_list;

shinyjs.init = function() {
  video = document.getElementById('time_lapse_vid');
  duration = document.getElementById('duration');
  time_elapsed = document.getElementById('time_elapsed');
  seek = document.getElementById('seek');
  progress_bar = document.getElementById('progress-bar');
  seekTooltip = document.getElementById('seek-tooltip');
  videoControls = document.getElementById('video-controls-wrapper');
  tl_button_wrapper = document.getElementById('time_lapse_radio-button-wrapper');
  tl_choice_list = document.getElementById('time-lapse-choice-list');
  console.log(window.getComputedStyle(tl_choice_list).getPropertyValue('height').replace('px', ''));
  tl_choice_list.style.maxHeight = window.getComputedStyle(tl_choice_list).getPropertyValue('height');
  console.log(tl_choice_list.style.maxHeight);
  vidControlsTimeoutId = window.setTimeout(function() {videoControls.classList.toggle('hid');}, hide_control_threshold);
  radioListTimeoutId = window.setTimeout(toggle_radio_buttons, hide_control_threshold);
  
  video.addEventListener(
    'ended', function() {Shiny.setInputValue('time_lapse_status', 'ended');}
  );
  video.addEventListener('loadedmetadata', initializeVideo);
  video.addEventListener('timeupdate', time_update);
  video.addEventListener('click', shinyjs.play_video);
  video.addEventListener('mousemove', function(e) {
    if (videoControls.className.search('hid') != -1) {videoControls.classList.toggle('hid');}
    window.clearTimeout(vidControlsTimeoutId);
    vidControlsTimeoutId = window.setTimeout(function() {videoControls.classList.toggle('hid');}, hide_control_threshold);
  });
  seek.addEventListener('mousemove', updateSeekTooltip);
  seek.addEventListener('mouseout', 
    function() {
      time_update();
      video.addEventListener('timeupdate', time_update);
      vidControlsTimeoutId = window.setTimeout(function() {videoControls.classList.toggle('hid');}, hide_control_threshold);
    }
  );
  seek.addEventListener('input', skipAhead);
  tl_button_wrapper.addEventListener('mouseenter', function() {
    window.clearTimeout(radioListTimeoutId);
    if (tl_button_wrapper.className.search('inactive') != -1) {toggle_radio_buttons();}
  });
  tl_button_wrapper.addEventListener('mouseleave', toggle_radio_buttons);
  document.querySelectorAll('input[name="time-lapse"]').forEach((el) => {
    el.addEventListener('change', function(event) {
      video.src = event.target.value;
      Shiny.setInputValue('time_lapse_status', 'playing');
    });
  });
};

shinyjs.play_video = function() {
  play_video();
};

play_video = function() {
  if (video.paused) {
    video.play();
    Shiny.setInputValue('time_lapse_status', 'playing');
  } else {
    video.pause();
    Shiny.setInputValue('time_lapse_status', 'paused');
  }
};
  
format_time = function(time_in_s) {
  const result = new Date(time_in_s * 1000).toISOString().substr(11, 8);
  return {
    minutes: result.substr(3, 2),
    seconds: result.substr(6, 2)
  };
};

initializeVideo = function() {
  const vidDuration = Math.round(video.duration);
  const time = format_time(vidDuration);
  duration.innerText = `${time.minutes}:${time.seconds}`;
  time_elapsed.innerText = '00:00';
  seek.setAttribute('max', vidDuration);
  progress_bar.setAttribute('max', vidDuration);
};

time_update = function() {
  const time = format_time(Math.round(video.currentTime));
  time_elapsed.innerText = `${time.minutes}:${time.seconds}`;
  seek.value = Math.floor(video.currentTime);
  progress_bar.value = Math.floor(video.currentTime);
  tooltip_step = (window.getComputedStyle(seek).getPropertyValue('width').replace('px', '') - window.getComputedStyle(time_elapsed).getPropertyValue('width').replace('px', '')) / video.duration;
  seekTooltip.style.left = `${video.currentTime * tooltip_step}px`;
  
};

updateSeekTooltip = function(event) {
  window.clearTimeout(vidControlsTimeoutId);
  video.removeEventListener('timeupdate', time_update);
  const skipTo = Math.round((event.offsetX / event.target.clientWidth) * parseInt(event.target.getAttribute('max'), 10));
  time_elapsed_width = window.getComputedStyle(time_elapsed).getPropertyValue('width').replace('px', '');
  dist_to_travel_ratio = 1 - time_elapsed_width / event.target.clientWidth;
  seek.setAttribute('data-seek', skipTo);
  const t = format_time(skipTo);
  time_elapsed.innerText = `${t.minutes}:${t.seconds}`;
  const rect = seek.getBoundingClientRect();
  seekTooltip.style.left = `${event.offsetX * dist_to_travel_ratio}px`;
};

skipAhead = function(event) {
  if (video.ended) {Shiny.setInputValue('time_lapse_status', 'paused');}
  const skipTo = event.target.dataset.seek ? event.target.dataset.seek : event.target.value;
  video.currentTime = skipTo;
  progress_bar.value = skipTo;
  seek.value = skipTo;
  video.addEventListener('timeupdate', time_update);
  vidControlsTimeoutId = window.setTimeout(function() {videoControls.classList.toggle('hid');}, hide_control_threshold);
};

toggle_radio_buttons = function() {
  tl_button_wrapper.classList.toggle('inactive');
  tl_choice_list.classList.toggle('inactive');
};