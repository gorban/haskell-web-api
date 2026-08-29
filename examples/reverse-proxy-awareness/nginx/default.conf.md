# nginx/default.conf

```nginx
location / {
    proxy_pass http://web_api_upstream;
    proxy_set_header Host $host;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header X-Forwarded-Proto https;
}
```

Use the existing `examples/reverse-proxy/nginx/default.conf` and `prefixed.conf` files as the
grounded runtime references for the current repo.
