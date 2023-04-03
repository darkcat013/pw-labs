"use strict";

import IdConstants from "./constants/id-constants.js";

const storageNotificationMinutesId = "notificationInterval"
let timerStarted = false

const newNotification = (notificationText) => {
  if (!("Notification" in window)) {
    alert("This browser does not support desktop notification");
  } else if (Notification.permission === "granted") {
    const notification = new Notification(notificationText);
  } else if (Notification.permission !== "denied") {
    Notification.requestPermission().then((permission) => {
      if (permission === "granted") {
        const notification = new Notification(notificationText);
      }
    });
  }
}

export const intervalChangedNotification = () => {
  const input = document.getElementById(IdConstants.notificationInput)
  const newMinutes = parseInt(input.value)
  if(isNaN(newMinutes) || newMinutes < 1) {
    alert("Minutes should be a valid positive integer number")
    input.value = ""
    return
  }
  localStorage.setItem(storageNotificationMinutesId, newMinutes)
  newNotification("Notification interval changed to: " + newMinutes + " minutes")

  !timerStarted && startNotificationTimer()
}

export const initNotificationInput = () => {
  const minutes = localStorage.getItem(storageNotificationMinutesId) || -1
  if (minutes == -1) return

  document.getElementById(IdConstants.notificationInput).value = minutes
  startNotificationTimer()
  timerStarted = true
}

const startNotificationTimer = () => {
  setTimeout(startNotificationTimer, parseInt(localStorage.getItem(storageNotificationMinutesId)) * 60000)
  newNotification("Have you completed all your tasks?")
}