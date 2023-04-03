"use strict";

import { switchMenu, switchTheme } from "./modules/switch-buttons.js";
import IdConstants from "./modules/constants/id-constants.js";
import { newTodo, initTodos, search } from "./modules/todos.js";
import { initTheme } from "./modules/theme.js";
import { removeAll, removeCompleted } from "./modules/local-storage-helper.js";
import { addDragDropEvents } from "./modules/drag-drop.js";

initTheme()

document.getElementById(IdConstants.themeSwitch).addEventListener("click", () => { switchTheme() })
document.getElementById(IdConstants.menuSwitch).addEventListener("click", () => { switchMenu() })

document.getElementById(IdConstants.newTaskBtn).addEventListener("click", () => { newTodo() })
document.getElementById(IdConstants.newTaskInput).addEventListener("keypress", (e) => {e.key === "Enter" && document.getElementById(IdConstants.newTaskBtn).click()})

document.getElementById(IdConstants.removeCompleted).addEventListener("click", () => {removeCompleted(); initTodos()})
document.getElementById(IdConstants.removeAll).addEventListener("click", () => {removeAll(); initTodos()})

document.getElementById(IdConstants.searchInput).addEventListener("keyup", (e) => {search(e.target.value)})

addDragDropEvents(document.getElementById(IdConstants.todosContainer))
addDragDropEvents(document.getElementById(IdConstants.completedTodosContainer))

initTodos()