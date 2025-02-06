SRC_DIR = src
OBJ_DIR = obj
BUILD_DIR = build
SRC = ${wildcard ${SRC_DIR}/*.asm}
OBJ = ${SRC:${SRC_DIR}/%.asm=${OBJ_DIR}/%.o}
OUT = game.nes
LCFG = linker.cfg

${BUILD_DIR}/${OUT}: ${OBJ}
	mkdir -p ${BUILD_DIR}
	ld65 -C linker.cfg -o $@ $^

${OBJ}: ${OBJ_DIR}/%.o : ${SRC_DIR}/%.asm
	mkdir -p ${OBJ_DIR}
	ca65 -o $@ $<

.PHONY: clean
clean:
	rm -rf ${BUILD_DIR} ${OBJ_DIR}