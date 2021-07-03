export function setLang(lang: any) {
  // window.localStorage.i18nextLng = lang;
  window.localStorage.i18nextLng =
    window.localStorage.i18nextLng === "en" ? "zh" : "en";
  window.location.reload();
}
