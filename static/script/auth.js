function login() {
  const username = document.getElementById("username").value;
  const password = document.getElementById("password").value;
  fetch("http://localhost:3000/api/login", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ username, password })
  })
  .then(response => response.json())
  .then(data => {
      if (data.user_id){
        localStorage.setItem("user_id", data.user_id);
      }
      document.getElementById("loginMessage").textContent = data.message || data.error;
  })
  .catch(error => console.error("Error:", error));
}

function logout() {
  localStorage.removeItem("user_id");
  fetch(`http://localhost:3000/api/logout`)
  .then(response => response.json())
  .then(data => {
    if (data.error){
      console.log(data.error);
    }
  })
  .catch(error => console.error("Error:", error));
}


function register() {
  const email = document.getElementById("email").value;
  const password = document.getElementById("password").value;
  fetch("http://localhost:3000/api/register", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ email, password })
  })
  .then(response => response.json())
  .then(data => {
      document.getElementById("registerMessage").textContent = data.message || data.error;
  })
  .catch(error => console.error("Error:", error));
}


function verify() {
  const v_code = document.getElementById("v_code").value;
  fetch(`http://localhost:3000/api/verify/${v_code}`)
  .then(response => response.json())
  .then(data => {
      document.getElementById("verifyMessage").textContent = data.message || data.error;
  })
  .catch(error => console.error("Error:", error));
}