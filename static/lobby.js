function createRoom() {
  const gameType = document.getElementById("gameType").value;
  fetch("http://localhost:3000/api/create-room", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ game_type: gameType })
  })
  .then(response => response.json())
  .then(data => {
      document.getElementById("roomMessage").textContent = data.lobby_room_id ? "Room Created: " + data.lobby_room_id : data.error;
  })
  .catch(error => console.error("Error:", error));
}