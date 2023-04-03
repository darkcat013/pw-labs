"use strict";

export const todosStorageId = "todos"

const updateStorage = (todos) => {
    localStorage.setItem(todosStorageId, JSON.stringify(todos))
}

export const getTodos = () => {
    return JSON.parse(localStorage.getItem(todosStorageId)) || []
}

const getTodoIndex = (id) => {
    return getTodos().findIndex((x) => x.id == id)
}

export const saveTodo = (todoObj) => {
    const todos = getTodos()
    todos.push(todoObj)
    updateStorage(todos)
}

export const removeTodo = (todoId) => {
    const filteredTodos = getTodos().filter((x) => x.id != todoId)
    updateStorage(filteredTodos)
}

export const updateTodo = (todoObj) => {
    const todos = getTodos()
    todos[getTodoIndex(todoObj.id)] = todoObj
    updateStorage(todos)
}

export const removeCompleted = () => {
    const filteredTodos = getTodos().filter((x) => !x.completed)
    updateStorage(filteredTodos)
}

export const removeAll = () => {
    localStorage.removeItem(todosStorageId)
}

