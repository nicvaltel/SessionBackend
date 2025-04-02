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


// function startRoomCheck(roomId) {
//   const checkInterval = setInterval(() => {
//       fetch(`http://localhost:3000/api/check-room/${roomId}`)
//       .then(response => response.json())
//       .then(data => {
//           if (data.room_id) {
//               clearInterval(checkInterval);
//               localStorage.removeItem("pendingRoomId");
//               window.location.href = `/game/${data.room_id}`;
//           }
//       })
//       .catch(error => console.error("Error:", error));
//   }, 1000);
// }


// document.addEventListener("DOMContentLoaded", () => {
//   const pendingRoomId = localStorage.getItem("pendingRoomId");
//   if (pendingRoomId) {
//       startRoomCheck(pendingRoomId);
//   }
// });