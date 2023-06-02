<script setup lang="ts">
import { ref } from 'vue';
import { useUserStore } from '../stores/user';
import music from '@/assets/just-relax-11157.mp3'
import { useQuizzesStore } from '@/stores/quizzes';

const burgerOn = ref(false)
const userStore = useUserStore()
const quizzesStore = useQuizzesStore()

</script>

<template>
  <nav class="navbar box p-0">
    <div class="navbar-brand">
      <a class="navbar-item" href="/">
        <span class="is-size-4"> Quiz App</span>
      </a>
      <div class="navbar-item is-size-6">
        {{ userStore.getNameSurname() }}
      </div>

      <div class="navbar-item">
        <audio :src="music" loop controls>
          Your browser does not support audio HTML tag
        </audio>
      </div>

      <a class="navbar-burger" :class="{ 'is-active': burgerOn }" @click="burgerOn = !burgerOn">
        <span></span>
        <span></span>
        <span></span>
      </a>
    </div>
    <div class="navbar-menu" :class="{ 'is-active': burgerOn }">

      <div class="navbar-end">
        <div class="navbar-item">
          <a class="button is-light is-bordered" @click="userStore.logOff(); quizzesStore.clear();">
            Log out
          </a>
        </div>
      </div>
    </div>
  </nav>
</template>