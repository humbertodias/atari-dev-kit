# ¿Qué es?

Servicio de CC65 para NES con Docker usando AlpineLinux.


# Prerrequisitos

1. [Docker](www.docker.com) 1.12+
2. [VirtualBox](www.virtualbox.org) 5.0+ (Necesario sólo para Mac o Windows)
3. [FCEUX](http://www.fceux.com/) 2.2.3+

# Cómo empezar

Construir

```
docker build -t hldtux/game-dev-kit-nes .
```

Correndo

```
docker run -t -v $PWD/src-games:/root/src-games -i hldtux/game-dev-kit-nes sh
cd src-games
```

Por último, compile

```
cl65 -t nes hello-nes.c -o hello.nes
```

Salida

```
fceux hello.nes
```

![](doc/output.png)


# Cómo detener

Tome el ID del CONTENEDOR

```
docker ps
```

```
CONTAINER ID        IMAGE                     COMMAND             CREATED              STATUS              PORTS               NAMES
529080b6b161        hldtux/game-dev-kit-nes   "sh"                About a minute ago   Up About a minute                       boring_hodgkin
```

Parada

```
docker stop 529080b6b161
```

o

Destruyendo

```
docker rm 529080b6b161
```

# ¿Cuanto cuesta?

Sólo ~77MB!

Dónde:

Paquete | MB
--- | ---
AlpineLinux | 4
CC65 | 73

Mostrar docker imágenes

```
docker images
```

```
REPOSITORY                TAG                 IMAGE ID            CREATED             SIZE
hldtux/game-dev-kit-nes   latest              8d717e697d42        51 seconds ago      77.3 MB
```

# Referencias

1. [docker.com](https://www.docker.com)

2. [hub.docker.com](https://hub.docker.com)

3. [alpinelinux.org](https://alpinelinux.org)

4. [cc65.org](http://www.cc65.org/)

5. [nesdoug.com](https://nesdoug.com/)