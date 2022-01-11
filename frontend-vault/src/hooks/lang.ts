export function setLang(lang: string) {
  window.localStorage.i18nextLng = lang
  // window.localStorage.i18nextLng =
  //   window.localStorage.i18nextLng === "zh" ? "en" : "zh";
  window.location.reload()
}
