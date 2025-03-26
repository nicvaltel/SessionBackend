function createRoom() {
  const gameType = document.getElementById("gameType").value;
  fetch("http://localhost:3000/api/create-room", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ game_type: gameType })
  })
  .then(response => response.json())
  .then(data => {
    if (data.lobby_room_id) {
        document.getElementById("roomMessage").textContent = "Room Created: " + data.lobby_room_id;
        localStorage.setItem("pendingRoomId", data.lobby_room_id);
        startRoomCheck(data.lobby_room_id);
    } else {
        document.getElementById("roomMessage").textContent = data.error;
    }
  })
  .catch(error => console.error("Error:", error));
}


function startRoomCheck(roomId) {
  const checkInterval = setInterval(() => {
      fetch(`http://localhost:3000/api/check-room/${roomId}`)
      .then(response => response.json())
      .then(data => {
          if (data.room_id) {
              clearInterval(checkInterval);
              localStorage.removeItem("pendingRoomId");
              window.location.href = `/game/${data.room_id}`;
          }
      })
      .catch(error => console.error("Error:", error));
  }, 1000);
}


document.addEventListener("DOMContentLoaded", () => {
  const pendingRoomId = localStorage.getItem("pendingRoomId");
  if (pendingRoomId) {
      startRoomCheck(pendingRoomId);
  }
});