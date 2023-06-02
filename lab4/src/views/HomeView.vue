<script setup lang="ts">
import { computed, onMounted, onUpdated, ref, watchEffect, type Ref, reactive } from 'vue';
import type { LocalStorageQuiz, QuizInfo } from '../interfaces/quiz';
import axiosUser from '@/plugins/axios-user';
import PlayIcon from '@/assets/play-button-svgrepo-com.svg';
import { useQuizzesStore } from '../stores/quizzes';
import type { HomeViewQuiz } from '../interfaces/quiz';
import type { Filter } from '../interfaces/filter';

const quizzes: Ref<HomeViewQuiz[]> = ref([])
const filters: Filter = reactive({
  ascending: -1,
  showCompleted: true,
  sortTitle: true
})

const page = ref(1)
const pageSize = 5
const maxPage = ref(1)

const byTitle = (a : HomeViewQuiz, b : HomeViewQuiz) => a.title < b.title ? filters.ascending : -filters.ascending
const byProgress = (a : HomeViewQuiz, b : HomeViewQuiz) => a.questionsSubmitted < b.questionsSubmitted ? filters.ascending : -filters.ascending

const filteredQuizzes = computed(() => {
  page.value = 1
  let result = quizzes.value
  let incompleted = result.filter(q => q.questionsSubmitted != q.questionsTotal)

  if (filters.sortTitle) {
    result = filters.showCompleted ? result : incompleted
    result = result.sort(byTitle)
  } else {
    incompleted = incompleted.sort(byProgress)
    if (filters.showCompleted){
      let completed = result.filter(q => q.questionsSubmitted == q.questionsTotal).sort(byProgress)
      result = [...incompleted, ...completed]
    }
    else result = incompleted
  }
  maxPage.value = Math.ceil(result.length / pageSize)
  return result
})

const pagedQuizzes = computed(() => {
  return filteredQuizzes.value.slice((page.value - 1) * pageSize, page.value * pageSize)
})

const isLoading = ref(true)

const quizzesStore = useQuizzesStore()

onMounted(async () => {
  isLoading.value = true
  const response = await axiosUser.get("quizzes")
  const quizzesInfo = response.data as QuizInfo[]

  const formattedQuizzes: HomeViewQuiz[] = []
  quizzesInfo.forEach(q => {
    const localQuiz = quizzesStore.getById(q.id)
    formattedQuizzes.push({
      id: q.id,
      title: q.title,
      titleClass: getQuizTitleClass(localQuiz),
      buttonClass: getQuizButtonClass(localQuiz),
      buttonText: getQuizButtonText(localQuiz),
      questionsTotal: q.questions_count,
      questionsSubmitted: localQuiz ? localQuiz.questionsSubmitted : 0,
      progressText: getQuizProgressText(localQuiz, q.questions_count)
    })
  });

  quizzes.value = formattedQuizzes
  isLoading.value = false
})

function getQuizTitleClass(localQuiz: LocalStorageQuiz | undefined) {
  if (!localQuiz) return "has-text-primary-dark"
  if (localQuiz.questionsSubmitted == localQuiz.questionsTotal) return "has-text-link-dark"
  return "has-text-warning-dark"
}

function getQuizButtonClass(localQuiz: LocalStorageQuiz | undefined) {
  if (!localQuiz) return "is-primary"
  if (localQuiz.questionsSubmitted == localQuiz.questionsTotal) return "is-link"
  return "is-warning"
}

function getQuizButtonText(localQuiz: LocalStorageQuiz | undefined) {
  if (!localQuiz) return "Play"
  if (localQuiz.questionsSubmitted == localQuiz.questionsTotal) return "Review"
  return "Continue"
}

function getQuizProgressText(localQuiz: LocalStorageQuiz | undefined, questionsCount: number) {
  const questionText = `${questionsCount == 1 ? "question" : "questions"}`
  if (!localQuiz) return `${questionsCount}  ${questionText}`
  return `Progress: ${localQuiz.questionsSubmitted}/${questionsCount} ${questionText}`
}


</script>


<template>
  <div class="container">
    <div class="level">
      <div class="level-left">
        <div class="level-item">
          <span class="is-size-2">Quizzes</span>
        </div>
      </div>
      <div class="level-right">
        <div class="level-item">
          <div class="level box p-3">
            <span class="level-item">Sort by:</span>
            <div class="control">
              <label class="radio">
                <input type="radio" name="sortBy" class="mr-1" v-model="filters.sortTitle" :value="true">
                <span>Title</span>
              </label>
              <label class="radio ">
                <input type="radio" name="sortBy" class="mr-1" v-model="filters.sortTitle" :value="false">
                <span>Progress</span>
              </label>
            </div>
          </div>
        </div>
        <div class="level-item">
          <div class="level box p-3">
            <div class="control">
              <label class="radio">
                <input type="radio" name="sort" checked v-model="filters.ascending" :value="-1">
                Asc
              </label>
              <label class="radio">
                <input type="radio" name="sort" v-model="filters.ascending" :value="1">
                Desc
              </label>
            </div>
          </div>
        </div>
        <div class="level-item">
          <div class="level box p-3">
            <label class="checkbox level-item">
              <input type="checkbox" class="mr-1" v-model="filters.showCompleted">
              <span>Show completed</span>
            </label>
          </div>
        </div>
      </div>
    </div>
    <div v-if="isLoading" class="box level is-mobile">
      <button class="button level-item is-secondary is-loading" disabled></button>
    </div>
    <div v-else-if="!pagedQuizzes.length">
      <div class="box has-text-centered"><span class="is-size-4"> No quizzes? 🤨</span></div>
    </div>
    <div v-else v-for="quiz in pagedQuizzes" class="box level is-mobile">
      <div class="level-left">
        <div class="level-item">
          <div>
            <a class="is-size-4 is-bold " :class="quiz.titleClass" :href="`/quiz/${quiz.id}`">{{ quiz.title }}</a>
            <p>{{ quiz.progressText }}</p>
          </div>
        </div>
      </div>
      <div class="level-right">
        <a class="button p-2 is-primary level-item is-hidden-mobile" :class="quiz.buttonClass" :href="`/quiz/${quiz.id}`">
          <img :src="PlayIcon" width="24" class="mr-1"><span>{{ quiz.buttonText }}</span></a>
      </div>
    </div>
    <div class="level is-mobile ">
      <div class="level-item">
        <a class="m-2" @click="page = 1">&lt;&lt;</a>
        <a class="m-2" @click="page > 1 && page--">&lt;</a>
        <span class="m-2">{{ pagedQuizzes.length ? page : 0 }}</span>
        <a class="m-2" @click="page < maxPage && page++">&gt;</a>
        <a class="m-2" @click="page = maxPage">&gt;&gt;</a>
      </div>
    </div>
  </div>
</template>
