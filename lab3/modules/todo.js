"use strict";

import SVG from "./constants/svg-strings.js";
import { appendTodo, newElementFromString } from "./element-helper.js";
import { removeTodo, updateTodo } from "./local-storage-helper.js";

export const newTodoElement = (todoObj) => {
    const div = newElementFromString('<div class="input-group mb-3" draggable="true"></div>')
    const textarea = newElementFromString('<textarea class="form-control col-10 col-xl-11" placeholder="Edit task" rows="1"></textarea>')
    textarea.value = todoObj.value
    textarea.disabled = todoObj.completed
    const buttonsDiv = newElementFromString('<div class="col-2 col-xl-1"></div>')
    const confirmBtn = newElementFromString(`<button class="btn ${todoObj.completed ? 'bg-warning' : 'bg-success'} text-bg-primary mb-2 ms-1" type="button" id="${todoObj.id}">${todoObj.completed ? SVG.uncheck : SVG.check}</button>`)
    const deleteBtn = newElementFromString('<button class="btn bg-danger text-bg-primary ms-1" type="button">' + SVG.delete + '</button>')
    buttonsDiv.append(confirmBtn, deleteBtn)
    div.append(textarea, buttonsDiv)

    textarea.addEventListener("keyup", (e) => {
        todoObj.value = e.target.value
        updateTodo(todoObj)
    })

    confirmBtn.addEventListener("click", () => {
        todoObj.completed = !todoObj.completed
        updateTodo(todoObj)
        div.remove()
        appendTodo(todoObj.completed, div)
        confirmBtn.removeChild(confirmBtn.firstChild)
        confirmBtn.append(newElementFromString(todoObj.completed ? SVG.uncheck : SVG.check))
        confirmBtn.className = `btn ${todoObj.completed ? 'bg-warning' : 'bg-success'} text-bg-primary mb-2 ms-1`
        textarea.disabled = todoObj.completed
    })

    deleteBtn.addEventListener("click", () => {
        div.remove()
        removeTodo(todoObj.id)
    })

    div.addEventListener("dragstart", (e) => {e.dataTransfer.setData('text/plain', todoObj.id)})

    return div;
}   
