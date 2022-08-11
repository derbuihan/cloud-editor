export const onRequestGet = async ({ env, params }) => {
    const key = params.id;
    const body = await env.files.get(key);
    return new Response(body);
};

export const onRequestPost = async ({ request, params, env }) => {
    const key = params.id;
    const text = await request.text();
    await env.files.put(key, text);
    return new Response(text);
};
