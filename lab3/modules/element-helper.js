"use strict";

import IdConstants from "./constants/id-constants.js";

export const newElementFromString = (htmlString) => {
    const wrapper = document.createElement('div')
    wrapper.innerHTML = htmlString;
    return wrapper.firstChild
}

export const appendTodo = (completed, todoElement) => {
    if(completed) document.getElementById(IdConstants.completedTodos).append(todoElement)
    else document.getElementById(IdConstants.todos).append(todoElement)
}