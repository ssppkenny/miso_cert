# Deployment Guide - Native Build

This guide shows how to deploy the working native build (jsaddle-warp version) of the cert quiz app.

## Quick Start (Local)

```bash
stack build
stack exec cert-exe
# Opens on http://localhost:3000
```

## Production Deployment Options

### Option 1: Simple Server Deployment

Deploy to any Linux server:

```bash
# Build statically linked binary
stack build --ghc-options='-optl-static -optl-pthread'

# Copy binary to server
scp .stack-work/install/.../bin/cert-exe user@server:/opt/cert-quiz/

# Run on server
ssh user@server
cd /opt/cert-quiz
PORT=8080 ./cert-exe
```

### Option 2: Docker Deployment

Create `Dockerfile`:

```dockerfile
FROM fpco/stack-build:lts as build
WORKDIR /opt/build
COPY . /opt/build
RUN stack build --system-ghc

FROM ubuntu:22.04
RUN apt-get update && apt-get install -y libgmp10 netbase
WORKDIR /opt/app
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/*/*/bin/cert-exe .
EXPOSE 3000
CMD ["./cert-exe"]
```

Build and run:
```bash
docker build -t cert-quiz .
docker run -p 3000:3000 cert-quiz
```

### Option 3: Heroku Deployment

Create `Procfile`:
```
web: cert-exe
```

Deploy:
```bash
heroku create my-cert-quiz
heroku buildpacks:add https://github.com/mfine/heroku-buildpack-stack
git push heroku main
```

### Option 4: DigitalOcean App Platform

1. Push to GitHub
2. Create new App on DigitalOcean
3. Connect GitHub repo
4. Set build command: `stack build`
5. Set run command: `stack exec cert-exe`
6. Deploy!

### Option 5: Nginx Reverse Proxy

For production with custom domain:

```nginx
server {
    listen 80;
    server_name quiz.example.com;

    location / {
        proxy_pass http://localhost:3000;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }
}
```

Run cert-exe as systemd service:

```ini
# /etc/systemd/system/cert-quiz.service
[Unit]
Description=Cert Quiz App
After=network.target

[Service]
Type=simple
User=certquiz
WorkingDirectory=/opt/cert-quiz
ExecStart=/opt/cert-quiz/cert-exe
Restart=always
Environment="PORT=3000"

[Install]
WantedBy=multi-user.target
```

Enable and start:
```bash
sudo systemctl enable cert-quiz
sudo systemctl start cert-quiz
```

## Environment Variables

- `PORT` - Port to listen on (default: 3000)

## Performance Tips

1. **Build with optimizations**:
   ```bash
   stack build --ghc-options='-O2'
   ```

2. **Use threading for better performance**:
   The app is already configured with `-threaded -rtsopts -with-rtsopts=-N`

3. **Monitor with systemd**:
   Automatic restarts on failure

4. **Use CDN for static assets**:
   Though this app has no static assets, for future additions

## Security Considerations

1. Run as non-root user
2. Use HTTPS (Let's Encrypt with certbot)
3. Keep dependencies updated: `stack update`
4. Monitor logs: `journalctl -u cert-quiz -f`

## Scaling

The app is stateless and can be scaled horizontally:

1. Run multiple instances behind load balancer
2. Use nginx or HAProxy for load balancing
3. LocalStorage is client-side only

## Monitoring

Add health check endpoint or use systemd watchdog:

```bash
# Check if app is running
curl http://localhost:3000/

# Check logs
journalctl -u cert-quiz --since "1 hour ago"
```

## Backup

No database to backup! Quiz data is embedded in the binary.
Only user progress is stored client-side in browser LocalStorage.

## Updates

To update:
```bash
git pull
stack build
sudo systemctl restart cert-quiz
```

## Cost Estimate

- DigitalOcean Droplet: $6/month (basic)
- Heroku: Free tier or $7/month  
- AWS EC2 t2.micro: ~$10/month
- Self-hosted: $0 (if you have a server)

## Conclusion

The native build is production-ready and can be deployed using standard Haskell/web app deployment practices. No special WASM considerations needed!

