export interface QuizInfo {
  id: number,
  title: string,
  questions_count: number
}

export interface Quiz {
  id: number,
  title: string,
  questions: Question[]
}

interface Question {
  id: number,
  question: string,
  answers: string[],
  answered_correctly: boolean,
  submitted_answer: string
}

export interface LocalStorageQuiz {
  id: number,
  questionsSubmitted: number,
  questionsTotal: number
}

export interface HomeViewQuiz {
  id: number,
  title: string,
  titleClass: string,
  buttonClass: string,
  buttonText: string,
  questionsSubmitted: number,
  questionsTotal: number,
  progressText: string
}