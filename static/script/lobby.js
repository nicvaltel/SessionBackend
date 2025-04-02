const socket = new WebSocket("ws://127.0.0.1:1234");

// At opening
socket.addEventListener("open", () => {
  console.log("Connected to WebSocket server");
  const sessionIdMsg = "session_id::" +  localStorage.getItem("session_id") + "\n";
  socket.send(sessionIdMsg);
});

// Input message processing
socket.addEventListener("message", (event) => {
  if (event.data.startsWith("guest_joined_room::")) {
    const roomId = event.data.substring("guest_joined_room::".length);
    console.log("Guest joined room:", roomId);
    window.location.href = `/game/${roomId}`;
  } else {
    console.log("Received:", event.data);
  }
});

// Errors processing
socket.addEventListener("error", (event) => {
  console.error("WebSocket error:", event);
});

// At closing
socket.addEventListener("close", () => {
  console.log("WebSocket connection closed");
});


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
        // localStorage.setItem("pendingRoomId", data.lobby_room_id);
        // startRoomCheck(data.lobby_room_id);
    } else {
        document.getElementById("roomMessage").textContent = data.error;
    }
  })
  .catch(error => console.error("Error:", error));
}


function updateLobby() {
    fetch("http://localhost:3000/api/lobby")
    .then(response => response.json())
    .then(data => {
        const lobbyList = document.getElementById("lobbyList");
        lobbyList.innerHTML = "";
        if (data.lobby_rooms_id) {
            data.lobby_rooms_id.forEach(roomId => {
                const li = document.createElement("li");
                const link = document.createElement("a");
                link.href = `game/${roomId}`;
                link.textContent = `Room ID: ${roomId}`;
                li.appendChild(link);
                lobbyList.appendChild(li);
            });
        } else {
            lobbyList.innerHTML = `<li>${data.error}</li>`;
        }
    })
    .catch(error => console.error("Error:", error));
}

setInterval(updateLobby, 1000);
