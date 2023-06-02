import { createRouter, createWebHistory } from 'vue-router'
import { useUserStore } from '@/stores/user'

const router = createRouter({
  history: createWebHistory(import.meta.env.BASE_URL),
  routes: [
    {
      path: '/',
      name: 'home',
      component: () => import('@/views/HomeView.vue')
    },
    {
      path: '/auth',
      name: 'auth',
      beforeEnter: (_to, _from) => {
        const userStore = useUserStore()
        if (userStore.isLoggedIn()) router.push('/')
      },
      component: () => import('@/views/AuthView.vue')
    },
    {
      path: '/quiz/:id',
      name: 'quiz',
      component: () => import('@/views/QuizView.vue')
    },
    {
      path: '/:pathMatch(.*)*',
      beforeEnter: (_to, _from) => router.push('/'),
      component: () => import('@/views/HomeView.vue')
    }
  ]
})

router.beforeEach((to, _from) => {
  const userStore = useUserStore()
  if (!userStore.isLoggedIn() && to.name !== 'auth') {
    return { name: 'auth' }
  }
})

export default router
