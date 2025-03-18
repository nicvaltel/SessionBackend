#!/bin/bash

URL="http://localhost:3000/api/login"

# Execute curl request and store the response
RESPONSE1=$(curl -s -X POST "$URL" -H "Content-Type: application/json" -d '{"username":"hello@mail.md", "password":"123456Hello"}')
RESPONSE2=$(curl -s -X POST "$URL" -H "Content-Type: application/json" -d '{"username":"hello2@mail.md", "password":"123456Hello2"}')

# Extract the session_token field from the JSON response
SESSION_TOKEN1=$(echo "$RESPONSE1" | jq -r '.session_token')
SESSION_TOKEN2=$(echo "$RESPONSE2" | jq -r '.session_token')

# Print the extracted session token
echo "Session Token1: $SESSION_TOKEN1"
echo "Session Token2: $SESSION_TOKEN2"

RESPONSE_CREATE_ROOM=$(curl -s -X POST http://localhost:3000/api/create-room --cookie "session_token=$SESSION_TOKEN1")
LOBBY_ROOM_ID=$(echo "$RESPONSE_CREATE_ROOM" | jq -r '.lobby_room_id')
echo "lobbyRoomId: $LOBBY_ROOM_ID"

RESPONSE_GET_LOBBY=$(curl -s -X GET http://localhost:3000/api/lobby --cookie "session_token=$SESSION_TOKEN2")
LOBBY_ALL_ROOMS=$(echo "$RESPONSE_GET_LOBBY" | jq -r '.lobby_rooms_id')
LOBBY_ALL_ROOMS_1=$(echo "$LOBBY_ALL_ROOMS" | jq -r '.[0]')
echo "all lobby rooms: $LOBBY_ALL_ROOMS"
echo "lobby first room: $LOBBY_ALL_ROOMS_1"


RESPONSE_JOIN_ROOM=$(curl -s -X GET http://localhost:3000/api/join-room/$LOBBY_ALL_ROOMS_1 --cookie "session_token=$SESSION_TOKEN2")
JOINED_ROOM=$(echo "$RESPONSE_JOIN_ROOM" | jq -r '.room_id')
echo "joined room: $JOINED_ROOM"

