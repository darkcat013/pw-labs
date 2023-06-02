import { ref } from 'vue'
import { defineStore } from 'pinia'
import axios from 'axios'

const TOKEN_STORAGE_ID = 'token'

interface LocalStorageToken {
  token: string,
  valid_before: Date,
  scopes: string[]
}

export const useTokenStore = defineStore('token', () => {
  const token = ref("")

  const storageToken = localStorage.getItem(TOKEN_STORAGE_ID)

  if (storageToken) {
    token.value = (JSON.parse(storageToken) as LocalStorageToken).token
  }

  async function updateToken() {
    const tokenResponse = await axios.post("https://late-glitter-4431.fly.dev/api/developers/v72/tokens", {},
      {
        headers: {
          "X-Developer-Key": import.meta.env.VITE_DEV_KEY,
          "X-Developer-Secret": import.meta.env.VITE_DEV_SECRET
        }
      }
    )
    localStorage.setItem(TOKEN_STORAGE_ID, JSON.stringify(tokenResponse.data))
    token.value = (tokenResponse.data as LocalStorageToken).token
    return token.value
  }

  return { token, updateToken }
})
