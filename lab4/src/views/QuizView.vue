<script setup lang="ts">
import axiosUser from '@/plugins/axios-user';
import { useUserStore } from '@/stores/user';
import { onMounted, ref, type Ref } from 'vue';
import { useRoute } from 'vue-router';
import type { Quiz } from '../interfaces/quiz';
import { useQuizzesStore } from '../stores/quizzes';

const isQuizLoading = ref(true)
const isSubmitLoading = ref(false)

const quizzesStore = useQuizzesStore()
const quiz: Ref<Quiz | undefined> = ref()
const currentQuestion = ref(0)

const route = useRoute()
const userStore = useUserStore()

const quizId = route.params.id
const userId = userStore.getUser().id

const currentAnswer = ref("")
const errorText = ref("")

onMounted(async () => {
  const response = await axiosUser.get(`quizzes/${quizId}?user_id=${userId}`)
  quiz.value = response.data as Quiz
  if (!quizzesStore.getById(quiz.value.id))
    quizzesStore.addNew({
      id: quiz.value.id,
      questionsSubmitted: 0,
      questionsTotal: quiz.value.questions.length
    })
  currentQuestion.value = quiz.value.questions.findIndex((el) => !el.submitted_answer)
  if (currentQuestion.value == -1) currentQuestion.value = quiz.value.questions.length
  isQuizLoading.value = false
})

function isFirstQuestion() {
  return currentQuestion.value == 0
}

function isLastQuestion() {
  return currentQuestion.value + 1 == quiz.value?.questions.length
}

function isScorePage() {
  return currentQuestion.value == quiz.value?.questions.length
}

function isQuizCompleted() {
  if (!quiz.value) return false
  const lastIndex = quiz.value.questions.length - 1
  return !!quiz.value.questions[lastIndex].submitted_answer
}

function isCurrentQuestionSubmitted() {
  if (!quiz.value) return false
  return !!quiz.value.questions[currentQuestion.value].submitted_answer
}

async function handleNextQuestionClick() {
  if(!quiz.value) {
    errorText.value ="An error has occured, cannot continue, please try later."
    return
  }
  if (!currentAnswer.value) {
    errorText.value = "Choose an answer first"
    return
  }

  try {
    isSubmitLoading.value = true
    const response = await axiosUser.post(`quizzes/${quizId}/submit`,
      {
        data: {
          question_id: quiz.value.questions[currentQuestion.value].id,
          answer: currentAnswer.value,
          user_id: userId
        }
      })
    quiz.value.questions[currentQuestion.value].submitted_answer = currentAnswer.value
    quiz.value.questions[currentQuestion.value].answered_correctly = response.data.correct
    currentQuestion.value++
    errorText.value = ""
    quizzesStore.updateById(quiz.value.id)
  } catch (error: any) {
    if (error.response.status == 400) {
      errorText.value = error.response.data.message.join('\n')
    } else {
      errorText.value = 'Network error, try again later'
    }
  } finally {
    currentAnswer.value = ""
    isSubmitLoading.value = false

  }
}

function handleNextQuestionClickCompleted() {
  currentQuestion.value++
  if (isScorePage()) return
  if (isQuizCompleted()) {
    currentAnswer.value = quiz.value!.questions[currentQuestion.value].submitted_answer
    return
  }
}

function handlePreviousQuestionClick() {
  currentQuestion.value--
  currentAnswer.value = quiz.value!.questions[currentQuestion.value].submitted_answer
}

function getScore() {
  let score = 0
  if (!quiz.value) return score
  quiz.value.questions.forEach((el) => {
    if (el.answered_correctly) score++
  })
  return score
}

</script>


<template>
  <div class="container mt-6">
    <div v-if="isQuizLoading" class="box level is-mobile">
      <button class="button level-item is-secondary is-loading" disabled></button>
    </div>
    <div v-else class="box columns is-mobile is-multiline">
      <div class="column is-full has-text-centered">
        <span class="is-size-1">{{ quiz?.title }}</span>
      </div>
      <div class="is-mobile is-multiline column columns is-full" v-if="isScorePage()">
        <div class="column is-full has-text-centered">
          <span class="is-size-3">Score: {{ getScore() }}/{{ quiz?.questions.length }}</span>
        </div>
      </div>
      <div class="is-mobile is-multiline column columns is-full" v-else>
        <div class="column is-full">
          <span class="is-size-3">{{ quiz?.questions[currentQuestion].question }}</span>
        </div>
        <div class="column is-full control " v-for="answer in quiz?.questions[currentQuestion].answers" :class="{
          'has-background-danger-light': isCurrentQuestionSubmitted() && answer == currentAnswer && !quiz?.questions[currentQuestion].answered_correctly,
          'has-background-primary-light': isCurrentQuestionSubmitted() && answer == currentAnswer && quiz?.questions[currentQuestion].answered_correctly
        }">
          <label class="radio">
            <input type="radio" name="answer" class="mr-2" :value="answer" v-model="currentAnswer"
              :disabled="isCurrentQuestionSubmitted()" :checked="isCurrentQuestionSubmitted() && answer == currentAnswer">
            <span class="is-size-5">{{ answer }}</span>
          </label>
        </div>
      </div>
      <div class="column is-full">
        <div class="level is-mobile">
          <div class="level-left">
            <div class="level-item">
              <button class="button is-light" :class="{ 'is-invisible': isFirstQuestion() || !isQuizCompleted() }"
                @click="!isFirstQuestion() && handlePreviousQuestionClick()">
                {{ "< Back" }}</button>
            </div>
          </div>
          <div class="level-item m-0">
            <a class="button is-danger" href="/">Exit</a>
          </div>
          <div class="level-right">
            <div class="level-item">
              <button class="button is-primary" :class="{ 'is-loading': isSubmitLoading, 'is-invisible': isScorePage() }"
                @click="isQuizCompleted() ? handleNextQuestionClickCompleted() : handleNextQuestionClick()">
                {{ !isLastQuestion() ? "Next >" : "Finish >" }}</button>
            </div>
          </div>
        </div>
      </div>
      <div class="column is-full">
        <div class="level is-mobile">
          <div class="level-item is-justify-content-flex-end">
            <div class="field help is-danger is-size-6" :class="{ 'is-hidden': !errorText }">
              {{ errorText }}
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

