# BQC REST API

Read-only HTTP API for querying Bluetooth device status from external tools.

## Configuration

Enable in **Settings > API** tab:
- **Enable REST API server**: Toggle the server on/off
- **Port**: HTTP port (default: 8765, range: 1024-65535)
- **Bind address**: Network interface (default: `127.0.0.1` for localhost only)

INI file section:
```ini
[API]
Enabled=0
Port=8765
BindAddress=127.0.0.1
```

## Endpoints

### GET /api/status

Returns adapter state and device summary counts.

**Response:**
```json
{
  "adapter": {
    "available": true,
    "enabled": true
  },
  "summary": {
    "totalDevices": 5,
    "connectedDevices": 2
  }
}
```

### GET /api/devices

Returns full device list with adapter state and summary.

**Response:**
```json
{
  "adapter": {
    "available": true,
    "enabled": true
  },
  "devices": [
    {
      "address": "AA:BB:CC:DD:EE:FF",
      "name": "Sony WH-1000XM5",
      "displayName": "My Headphones",
      "type": "AudioOutput",
      "connectionState": "Connected",
      "isConnected": true,
      "isPaired": true,
      "isPinned": true,
      "battery": {
        "level": 85,
        "supported": true,
        "text": "85%"
      },
      "lastSeen": "2026-02-21T15:30:00",
      "profiles": ["A2DP", "HFP", "AVRCP"]
    }
  ],
  "summary": {
    "totalDevices": 5,
    "connectedDevices": 2
  }
}
```

### GET /api/devices/{address}

Returns a single device by Bluetooth address.

**Address formats:**
- `AA:BB:CC:DD:EE:FF` (colon-separated)
- `AA-BB-CC-DD-EE-FF` (dash-separated)
- `AABBCCDDEEFF` (compact hex)

**Response:** Single device object (same structure as array element above).

**404 Response:**
```json
{
  "error": "Device not found",
  "code": 404
}
```

## Field Reference

| Field | Type | Description |
|-------|------|-------------|
| `address` | string | MAC address (XX:XX:XX:XX:XX:XX) |
| `name` | string | Bluetooth device name |
| `displayName` | string | User alias or device name |
| `type` | string | Device type enum (see below) |
| `connectionState` | string | Connection state enum (see below) |
| `isConnected` | boolean | Whether device is currently connected |
| `isPaired` | boolean | Whether device is paired |
| `isPinned` | boolean | Whether device is pinned to top |
| `battery.level` | number/null | Battery percentage (0-100) or null |
| `battery.supported` | boolean | Whether device reports battery |
| `battery.text` | string | Formatted battery text |
| `lastSeen` | string/null | ISO 8601 timestamp or null |
| `profiles` | string[] | Active Bluetooth profile short names |

**Device types:** `Unknown`, `AudioOutput`, `AudioInput`, `Headset`, `Computer`, `Phone`, `Keyboard`, `Mouse`, `Gamepad`, `HID`

**Connection states:** `Disconnected`, `Connected`, `Connecting`, `Disconnecting`, `Unknown`, `Error`

## Notes

- All endpoints are read-only (GET only)
- CORS headers (`Access-Control-Allow-Origin: *`) are included for web dashboard use
- No authentication -- restrict access via bind address
- Snapshots are rebuilt on every device state change in the main application
