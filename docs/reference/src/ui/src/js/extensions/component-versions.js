;(function () {
  'use strict'

  var toggle = document.querySelector('.page-components .component-menu-toggle')
  if (!toggle) return

  var selector = document.querySelector('.page-components')

  toggle.addEventListener('click', function (e) {
    selector.classList.toggle('is-active')
    e.stopPropagation() // trap event
  })

  document.documentElement.addEventListener('click', function () {
    selector.classList.remove('is-active')
  })
})()
