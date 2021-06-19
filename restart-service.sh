./stop-services.sh

# call with --dev to run in debug mdoe

if [ "$1" = "--dev" ]; then
  make docker-compose-dev
else
  make docker-compose
fi
