<script setup lang="ts">
import axiosUser from '@/plugins/axios-user';
import router from '@/router';
import { useUserStore } from '@/stores/user';
import { reactive, ref } from 'vue';
import type { User } from '../interfaces/user';
import { useQuizzesStore } from '../stores/quizzes';
import type { LocalStorageQuiz, Quiz, QuizInfo } from '@/interfaces/quiz';

const formInput = reactive({
  name: '',
  surname: ''
})

const errorText = ref('')
const loginLoading = ref(false)
const registerLoading = ref(false)

function validateInput() {
  if (!(formInput.name && formInput.surname)) {
    errorText.value = 'Invalid name or surname'
    return false
  }
  return true
}

async function initUserQuizzes(user: User) {
  const quizzesStore = useQuizzesStore()
  const response = await axiosUser.get("quizzes")
  const quizzes = response.data as QuizInfo[]
  const userQuizzes: LocalStorageQuiz[] = []
  for (let i = 0; i < quizzes.length; i++) {
    const quizInfo = quizzes[i];
    const response = await axiosUser.get(`quizzes/${quizInfo.id}?user_id=${user.id}`)
    const quiz = response.data as Quiz

    let submitted = 0
    quiz.questions.forEach(q => {
      if (q.submitted_answer) submitted++
    })

    if(submitted)
    userQuizzes.push({
      id: quiz.id,
      questionsSubmitted: submitted,
      questionsTotal: quiz.questions.length
    })
  }

  quizzesStore.set(userQuizzes)
}

async function updateLocalUser(user: User) {
  const userStore = useUserStore()
  userStore.updateUser(user)
  router.push('/')
}

async function handleLogin(e: any) {
  if (!validateInput()) return

  try {
    loginLoading.value = true
    const allUsers = await axiosUser.get('users')
    const user = (allUsers.data as User[]).find((x) => x.name == formInput.name && x.surname == formInput.surname)
    if (!user) {
      errorText.value = 'Name and surname not found, check if they are correct'
    } else {
      await initUserQuizzes(user)
      await updateLocalUser(user)
    }
  }
  catch {
    errorText.value = 'Network error, try again later'
  }
  finally {
    loginLoading.value = false
  }

}

async function handleRegister(e: any) {
  if (!validateInput()) return

  try {
    registerLoading.value = true
    const userResponse = await axiosUser.post('users', { data: formInput })
    const user = userResponse.data as User
    await initUserQuizzes(user)
    await updateLocalUser(user)
  }
  catch (error: any) {
    if (error.response?.status == 400) {
      errorText.value = error.response.data.message.join('\n')
    } else {
      errorText.value = 'Network error, try again later'
    }
  }
  finally {
    registerLoading.value = false
  }
}

</script>

<template>
  <div class="is-screen-height level is-flex-mobile">
    <div class="level-item is-flex-grow-1 p-2">
      <form>
        <div class="field has-text-centered">
          <span class="is-size-2">Authentication</span>
        </div>
        <div class="field">
          <label class="label">Name</label>
          <div class="control">
            <input class="input" :class="{ 'is-danger': errorText }" type="text" placeholder="Enter your name"
              v-model.trim="formInput.name">
          </div>
        </div>
        <div class="field">
          <label class="label">Surname</label>
          <div class="control">
            <input class="input" :class="{ 'is-danger': errorText }" type="text" placeholder="Enter your surname"
              v-model.trim="formInput.surname">
          </div>
        </div>
        <div class="field help is-danger is-size-6" :class="{ 'is-hidden': !errorText }">
          {{ errorText }}
        </div>
        <div class="field">
          <div class="columns">
            <div class="column is-flex is-justify-content-center">
              <button class="button is-primary is-fullwidth" :class="{ 'is-loading': loginLoading }"
                @click.prevent="handleLogin">Login</button>
            </div>
            <div class="column is-flex is-justify-content-center">
              <button class="button is-primary is-outlined is-fullwidth" :class="{ 'is-loading': registerLoading }"
                @click.prevent="handleRegister">Register</button>
            </div>
          </div>
        </div>
      </form>
    </div>
  </div>
</template>

<style scoped lang="scss">
form {
  max-width: 400px;
  width: 100%;
}

.is-screen-height {
  height: 100vh;
}
</style>