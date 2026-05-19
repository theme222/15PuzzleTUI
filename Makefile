linux: # Yes really apparently the industry standard is to compile it statically inside a docker container. (Me when haskell devs)
	@echo "Building for linux..."
	docker build -t 15p .
	@echo "Cleaning up any old containers..."
	-docker rm -f 15p-container 2>/dev/null || true
	@echo "Extracting binary..."
	docker create --name 15p-container 15p
	docker cp 15p-container:/app/15p ./15p
	@echo "Removing temporary container..."
	docker rm -f 15p-container

clean:
	@echo "Cleaning..."
	docker rmi -f 15p
	-docker rm -f 15p-container 2>/dev/null || true
	rm -f 15p