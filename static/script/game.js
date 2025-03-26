function joinGame(gId) {
  fetch(`http://localhost:3000/api/join-room/${gId}`)
  .then(response => response.json())
  .catch(error => console.error("Error:", error));
}

const gameId = window.location.pathname.split("/").pop();
document.getElementById("game-id").textContent = gameId;
joinGame(gameId);