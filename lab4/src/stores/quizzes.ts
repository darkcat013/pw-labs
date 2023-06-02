import { defineStore } from 'pinia'
import type { LocalStorageQuiz } from '../interfaces/quiz';
import { ref, type Ref } from 'vue';

const QUIZZES_STORAGE_ID = 'quizzes'

export const useQuizzesStore = defineStore('quizzes', () => {
  const quizzes : Ref<LocalStorageQuiz[]> = ref([])

  const storageQuizzes = localStorage.getItem(QUIZZES_STORAGE_ID)

  if (storageQuizzes) {
    quizzes.value = (JSON.parse(storageQuizzes) as LocalStorageQuiz[])
  }

  function set(newQuizzes: LocalStorageQuiz[]) {
    quizzes.value = newQuizzes
    updateStorage()
  }

  function updateStorage() {
    localStorage.setItem(QUIZZES_STORAGE_ID, JSON.stringify(quizzes.value))
  }

  function getById(id: number) {
    return quizzes.value.find(q => q.id == id)
  }

  function updateById(id: number) {
    const quizIndex = quizzes.value.findIndex(q => q.id == id)
    quizzes.value[quizIndex].questionsSubmitted++
    updateStorage()
  }

  function addNew(quiz: LocalStorageQuiz) {
    quizzes.value.push(quiz)
    updateStorage()
  }

  function clear() {
    localStorage.removeItem(QUIZZES_STORAGE_ID)
  }

  return { set, getById, updateById, addNew, clear}
})
