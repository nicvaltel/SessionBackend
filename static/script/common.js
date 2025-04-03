function getCookie(name) {
  console.log(document.cookie);
  let x = document.getCookie;
  console.log(x);
  const value = `; ${document.cookie}`;
  const parts = value.split(`; ${name}=`);
  if (parts.length === 2) return parts.pop().split(';').shift();
}
