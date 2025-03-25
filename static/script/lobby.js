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


// function updateLobby() {
//   fetch("http://localhost:3000/api/lobby")
//   .then(response => response.json())
//   .then(data => {
//       const lobbyList = document.getElementById("lobbyList");
//       lobbyList.innerHTML = "";
//       if (data.lobby_rooms_id) {
//           data.lobby_rooms_id.forEach(roomId => {
//               const li = document.createElement("li");
//               li.textContent = "Room ID: " + roomId;
//               lobbyList.appendChild(li);
//           });
//       } else {
//           lobbyList.innerHTML = `<li>${data.error}</li>`;
//       }
//   })
//   .catch(error => console.error("Error:", error));
// }
