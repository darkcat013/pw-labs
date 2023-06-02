import { useTokenStore } from '@/stores/token'
import axios from 'axios'

const API_BASE_URL = "https://late-glitter-4431.fly.dev/api/v54/"

const axiosUser = axios.create()

axiosUser.interceptors.request.use(async (config) => {
  const tokenStore = useTokenStore()
  config.baseURL = API_BASE_URL
  config.headers['X-Access-Token'] = tokenStore.token
  return config
})

axiosUser.interceptors.response.use(
  response => Promise.resolve(response),
  async error => {
    const originalConfig = error.config;
    const tokenStore = useTokenStore()

    if (error.response) {
      // Access Token expired or not set up
      if (error.response.status === 401 && !originalConfig._retry) {
        originalConfig._retry = true;

        //Retry request after updating token
        try {
          originalConfig.headers['X-Access-Token'] = await tokenStore.updateToken();
          return axiosUser(originalConfig)
        } catch (_error: any) {
          if (_error.response && _error.response.data) {
            return Promise.reject(_error.response.data);
          }

          return Promise.reject(_error);
        }
      }

      // reject after second retry
      if (error.response.status === 403 && error.response.data) {
        return Promise.reject(error.response.data);
      }
    }

    return Promise.reject(error)
  }
)

export default axiosUser