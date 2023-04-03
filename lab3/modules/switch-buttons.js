"use strict";
import IdConstants from "./constants/id-constants.js"
import { darkThemeId, initTheme } from "./theme.js";

export const switchTheme = () => {
  localStorage.setItem(darkThemeId, !JSON.parse(localStorage.getItem(darkThemeId)))
  initTheme()
}

export const switchMenu = () => {
  const sidenav = document.getElementById(IdConstants.sideNav)
  let width = sidenav.style.width;
  if (width === "0px" || width == "") {
    sidenav.style.width = "250px";
  }
  else {
    sidenav.style.width = "0px";
  }
}
