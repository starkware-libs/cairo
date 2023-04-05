/* eslint-disable */
;(function () {
  'use strict'
  var images = document.querySelectorAll('img')

  function hasParentClass(child, classname){
    if(child.className.split(' ').indexOf(classname) >= 0) return true;
    try{
      //Throws TypeError if child doesn't have parent any more
      return child.parentNode && hasParentClass(child.parentNode, classname);
    }catch(TypeError){
      return false;
    }
  }

  for (var i = 0; i < images.length; i++) {
    if (!(images[i].classList.contains('copy-icon') || images[i].classList.contains('pencil') || hasParentClass(images[i],'overview-box'))){
      images[i].setAttribute('data-zoomable', 'true')
    }
  }

  mediumZoom('[data-zoomable]');

  let theme = window.localStorage.getItem('data-theme');

  if (theme.length) {
    set_style_from_localstorage(theme);
  } else {
    set_style_from_localstorage('Default');
  }
})()

function switch_style (css_title)
{
  var i, link_tag ;
  if (css_title == 'toggle') {
    if (window.localStorage.getItem('data-theme') == 'Default') {
      css_title = 'Dark';
    } else {
      css_title = 'Default';
    }
  }
  for (i = 0, link_tag = document.getElementsByTagName("link");
       i < link_tag.length ; i++ ) {
    if ((link_tag[i].rel.indexOf( "stylesheet" ) != -1) &&
      link_tag[i].title) {
      link_tag[i].disabled = true ;
      if (link_tag[i].title == css_title) {
        link_tag[i].disabled = false ;
      }
    }
    set_localstorage( css_title);
  }

  let themeName = css_title === 'Dark' ? 'dark': 'light'
  document.documentElement.setAttribute('data-theme', themeName)
};

function set_style_from_localstorage(css_title)
{
  if (!css_title) { css_title = "Default"};
  if (css_title.length) {
    switch_style( css_title );
  }
}

function set_localstorage (title) {
  if (title == "Dark") {
    window.localStorage.setItem('data-theme', 'Dark');
  } else {
    window.localStorage.setItem('data-theme', 'Default');
  }
}
