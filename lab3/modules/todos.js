"use strict";

import IdConstants from "./constants/id-constants.js";
import { appendTodo } from "./element-helper.js";
import { getTodos, saveTodo } from "./local-storage-helper.js";
import { newTodoElement } from "./todo.js";

const resetTodos = () => {
    document.getElementById(IdConstants.completedTodos).innerHTML=""
    document.getElementById(IdConstants.todos).innerHTML=""
}

export const initTodos = () => {
    resetTodos()
    getTodos().forEach((x) => {appendTodo(x.completed, newTodoElement(x))})
}

export const newTodo = () => {
    const input = document.getElementById(IdConstants.newTaskInput)
    const value = input.value;
    if(value == "" || value == undefined) {
        alert("Task text should not be empty")
        return
    }
    input.value = ""
    const todoId = self.crypto.randomUUID()
    const todoObj = {id: todoId, value: value, completed: false}
    saveTodo(todoObj)
    document.getElementById(IdConstants.todos).append(newTodoElement(todoObj))
}

export const search = (value) => {
    if (value == "" || value == undefined) {
        initTodos()
        return
    }
    resetTodos()
    getTodos().forEach((x) => {
        x.value.toLowerCase().includes(value.toLowerCase()) && appendTodo(x.completed, newTodoElement(x))
    })
}
