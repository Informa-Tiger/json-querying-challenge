# Backend

* Prequesites: `cabal`
* Build: `cabal build`
* Run: `cabal run json-querying-challenge.cabal -- JSON_TABLE [DB_FILE]`
* Configuration: 
  - enviroment variable: `PORT` (default: `8000`)

# Frontend
* Prequesites: `npm`
* Build: `npm run build`
* Run: `npm run dev`
* Configuration:
  - .env file: `PUBLIC_API` (address of backend, default: `http://localhost:8000`)