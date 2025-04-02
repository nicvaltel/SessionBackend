const socket = new WebSocket("ws://127.0.0.1:1234");

// At opening
socket.addEventListener("open", () => {
  console.log("Connected to WebSocket server");
  socket.send("hello");
});

// Input message processing
socket.addEventListener("message", (event) => {
  console.log("Received:", event.data);
});

// Errors processing
socket.addEventListener("error", (event) => {
  console.error("WebSocket error:", event);
});

// At closing
socket.addEventListener("close", () => {
  console.log("WebSocket connection closed");
});
