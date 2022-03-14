import React from "react"

interface Props {
  fill?: string
}
const LinkedinIcon: React.FunctionComponent<Props> = ({ fill = "#F5FCFE" }) => {
  return (
    <svg
      width="21"
      height="21"
      viewBox="0 0 21 21"
      fill="none"
      xmlns="http://www.w3.org/2000/svg"
    >
      <g clip-path="url(#clip0_3482_17171)">
        <path
          d="M20.6453 20.3446V20.3438H20.6503V13.0088C20.6503 9.42042 19.8778 6.65625 15.6828 6.65625C13.6662 6.65625 12.3128 7.76292 11.7603 8.81209H11.702V6.99125H7.72449V20.3438H11.8662V13.7321C11.8662 11.9913 12.1962 10.3079 14.352 10.3079C16.4762 10.3079 16.5078 12.2946 16.5078 13.8438V20.3446H20.6453Z"
          fill={fill}
        />
        <path
          d="M0.980225 6.99219H5.12689V20.3447H0.980225V6.99219Z"
          fill={fill}
        />
        <path
          d="M3.05194 0.344727C1.7261 0.344727 0.650269 1.42056 0.650269 2.74639C0.650269 4.07223 1.7261 5.17056 3.05194 5.17056C4.37777 5.17056 5.4536 4.07223 5.4536 2.74639C5.45277 1.42056 4.37694 0.344727 3.05194 0.344727V0.344727Z"
          fill={fill}
        />
      </g>
      <defs>
        <clipPath id="clip0_3482_17171">
          <rect
            width="20"
            height="20"
            fill="white"
            transform="translate(0.650269 0.344727)"
          />
        </clipPath>
      </defs>
    </svg>
  )
}

export default LinkedinIcon