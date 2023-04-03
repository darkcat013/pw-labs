"use strict";

export const addDragDropEvents = (element) => {
    element.addEventListener('dragenter', (e) => {dragEnter(e, element)})
    element.addEventListener('dragover', (e) => {dragOver(e, element)});
    element.addEventListener('dragleave', (e) => {dragLeave(e, element)});
    element.addEventListener('drop', (e) => {drop(e, element)});
}

function dragEnter(e, element) {
    e.preventDefault();
    element.classList.add('drag-over');
}

function dragOver(e, element) {
    e.preventDefault();
    element.classList.add('drag-over');
}

function dragLeave(e, element) {
    element.classList.remove('drag-over');
}

function drop(e, element) {
    element.classList.remove('drag-over');
    const id = e.dataTransfer.getData('text/plain');
    document.getElementById(id).click();
}