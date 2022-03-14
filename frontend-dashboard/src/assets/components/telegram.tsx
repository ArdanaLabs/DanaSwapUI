import React from "react"

interface Props {
  fill?: string
}
const TelegramIcon: React.FunctionComponent<Props> = ({ fill = "#F5FCFE" }) => {
  return (
    <svg
      width="26"
      height="22"
      viewBox="0 0 26 22"
      fill="none"
      xmlns="http://www.w3.org/2000/svg"
    >
      <path
        d="M9.86755 14.0754L9.454 19.892C10.0457 19.892 10.3019 19.6379 10.6092 19.3326L13.3832 16.6816L19.1311 20.891C20.1853 21.4785 20.928 21.1691 21.2123 19.9212L24.9853 2.24202L24.9863 2.24098C25.3207 0.682649 24.4228 0.073274 23.3957 0.455566L1.21859 8.94619C-0.294956 9.53369 -0.27204 10.3774 0.961294 10.7597L6.63109 12.5233L19.8009 4.28265C20.4207 3.87223 20.9842 4.09932 20.5207 4.50973L9.86755 14.0754Z"
        fill={fill}
      />
    </svg>
  )
}

export default TelegramIcon
