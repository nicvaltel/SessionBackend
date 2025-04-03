#!/bin/bash

# Execute curl request and store the response
URL_LOGIN="http://localhost:3000/api/login"
RESPONSE1=$(curl -s -X POST "$URL_LOGIN" -H "Content-Type: application/json" -d '{"username":"hello@mail.md", "password":"123456Hello"}')
RESPONSE2=$(curl -s -X POST "$URL_LOGIN" -H "Content-Type: application/json" -d '{"username":"hello2@mail.md", "password":"123456Hello2"}')

# Extract the session_id field from the JSON response
SESSION_ID1=$(echo "$RESPONSE1" | jq -r '.session_id')
SESSION_ID2=$(echo "$RESPONSE2" | jq -r '.session_id')

# Print the extracted session token
echo "Session Token1: $SESSION_ID1"
echo "Session Token2: $SESSION_ID2"

URL_CREATE_ROOM="http://localhost:3000/api/create-room"
RESPONSE_CREATE_ROOM=$(curl -s -X POST "$URL_CREATE_ROOM" -H "Content-Type: application/json" -d '{"game_type":"casual"}' --cookie "session_id=$SESSION_ID1")
LOBBY_ROOM_ID=$(echo "$RESPONSE_CREATE_ROOM" | jq -r '.lobby_room_id')
echo "lobbyRoomId: $LOBBY_ROOM_ID"

RESPONSE_GET_LOBBY=$(curl -s -X GET http://localhost:3000/api/lobby --cookie "session_id=$SESSION_ID2")
LOBBY_ALL_ROOMS=$(echo "$RESPONSE_GET_LOBBY" | jq -r '.lobby_rooms_id')
LOBBY_ALL_ROOMS_1=$(echo "$LOBBY_ALL_ROOMS" | jq -r '.[0]')
echo "all lobby rooms: $LOBBY_ALL_ROOMS"
echo "lobby first room: $LOBBY_ALL_ROOMS_1"


RESPONSE_JOIN_ROOM=$(curl -s -X GET http://localhost:3000/api/join-room/$LOBBY_ALL_ROOMS_1 --cookie "session_id=$SESSION_ID2")
JOINED_ROOM=$(echo "$RESPONSE_JOIN_ROOM" | jq -r '.room_id')
echo "joined room: $JOINED_ROOM"


# WebSocket-сервер и сообщение
WS_URL="ws://localhost:1234"
WS_SESSION_MSG1="session_id::$SESSION_ID1"

# Отправляем сообщение и получаем ответ
echo "Отправка: $WS_SESSION_MSG1"
WS_RESPONSE1=$(websocat -U -1 -t "$WS_URL" <<< "$WS_SESSION_MSG1")

# Выводим ответ
echo "Ответ сервера: $WS_RESPONSE1"
