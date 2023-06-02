import { ref } from 'vue'
import { defineStore } from 'pinia'
import router from '@/router'
import type { User } from '@/interfaces/user'

const USER_STORAGE_ID = 'user'

export const useUserStore = defineStore('user', () => {
  const initialValue: User = {
    id: 0,
    name: '',
    surname: ''
  }
  const user = ref(initialValue)

  const storageUser = localStorage.getItem(USER_STORAGE_ID)

  if (storageUser) {
    user.value = (JSON.parse(storageUser) as User)
  }

  function getUser() {
    return user.value
  }

  function updateUser(newUser: User) {
    localStorage.setItem(USER_STORAGE_ID, JSON.stringify(newUser))
    user.value = newUser
  }

  function isLoggedIn() {
    return !!(user.value.name && user.value.surname)
  }

  function logOff() {
    localStorage.removeItem(USER_STORAGE_ID)
    user.value = initialValue
    router.push("/auth")
  }

  function getNameSurname() {
    return `${user.value.name} ${user.value.surname}`
  }

  return { getUser, updateUser, isLoggedIn, logOff, getNameSurname }
})
