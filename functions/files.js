export const onRequestGet = async ({ env }) => {
    const files = (await env.files.list()).keys.map((key) => {
        return key.name;
    });
    const res = JSON.stringify(files);
    return new Response(res);
};
