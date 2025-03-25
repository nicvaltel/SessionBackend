const gameId = window.location.pathname.split("/").pop();
document.getElementById("game-id").textContent = gameId;