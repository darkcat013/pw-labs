"use strict";

import IdConstants from "./constants/id-constants.js";

export const darkThemeId = "darkTheme"

export const initTheme = () => {
    if (JSON.parse(localStorage.getItem(darkThemeId))) {
        document.documentElement.setAttribute('data-bs-theme', 'dark')
        document.getElementById(IdConstants.themeSwitch).checked = true
    } else {
        document.documentElement.setAttribute('data-bs-theme', 'light')
        document.getElementById(IdConstants.themeSwitch).checked = false
    }
}