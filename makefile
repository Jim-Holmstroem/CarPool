redis-cli-exec = docker run -it --rm --net=host redis redis-cli


build:
	stack build
run: build
	stack exec CarPool

redis-populate:
	$(redis-cli-exec) sadd users:kia jim liza anna sofia

redis-clear:
	docker rm -f redis || true

redis: redis-clear
	docker run --name redis -d --net=host redis redis-server

redis-cli:
	$(redis-cli-exec)
