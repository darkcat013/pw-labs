<script setup lang="ts">
import { RouterView } from 'vue-router'
import { useUserStore } from './stores/user'
import Navbar from '@/components/Navbar.vue'

const userStore = useUserStore()
</script>

<template>
  <Navbar v-if="userStore.isLoggedIn()" />
  <!-- <nav >
        <RouterLink to="/">Home</RouterLink>
        <RouterLink to="/about">About</RouterLink>
      </nav> -->
  <main>
    <RouterView />
  </main>
</template>
